package scalashop

import java.util.concurrent.ForkJoinTask

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

	val standardConfig = config(
		Key.exec.minWarmupRuns -> 5,
		Key.exec.maxWarmupRuns -> 10,
		Key.exec.benchRuns -> 10,
		Key.verbose -> true
	) withWarmer (new Warmer.Default)

	def main(args: Array[String]): Unit = {
		val radius = 4
		val width = 1920
		val height = 1080
		val src = new Img(width, height)
		val dst = new Img(width, height)
		val seqtime = standardConfig measure {
			VerticalBoxBlur.blur(src, dst, 0, width, radius)
		}
		println(s"sequential blur time: $seqtime ms")

		val numTasks = 16
		val partime = standardConfig measure {
			VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
		}
		println(s"fork/join blur time: $partime ms")
		println(s"speedup: ${seqtime / partime}")
	}

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

	/** Blurs the columns of the source image `src` into the destination image
	 * `dst`, starting with `from` and ending with `end` (non-inclusive).
	 *
	 * Within each column, `blur` traverses the pixels by going from top to
	 * bottom.
	 */
	def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
		for (column <- from until end; row <- 0 until src.height) {
			dst.update(column, row, boxBlurKernel(src, column, row, radius))
		}
	}

	/** Blurs the columns of the source image in parallel using `numTasks` tasks.
	 *
	 * Parallelization is done by stripping the source image `src` into
	 * `numTasks` separate strips, where each strip is composed of some number of
	 * columns.
	 */
	def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
		val leftover = src.width % numTasks
		val bucket = src.width / numTasks


		val planoutInit: List[(Int, Int)] = (0 until numTasks).map((
			taskId: Int) => (taskId * bucket, (taskId * bucket) + bucket)).toList

		val planoutAll = (planoutInit.head._1, planoutInit.head._2 + leftover) :: planoutInit.tail

		planoutAll.map { case (start: Int, end: Int) => task(blur(src, dst, start, end, radius)) }.map {
			(fork: ForkJoinTask[Unit]) => fork.join()
		}
	}

}
